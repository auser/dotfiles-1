#!/usr/bin/env python3

# Copyright 2019 Amazon.com, Inc. or its affiliates. All Rights Reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License").
# You may not use this file except in compliance with the License.
# A copy of the License is located at
#
# http://aws.amazon.com/apache2.0/
#
# or in the "LICENSE.txt" file accompanying this file.
# This file is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
# CONDITIONS OF ANY KIND, express or implied. See the License for the
# specific language governing permissions and # limitations under the License.

import argparse
import distutils.spawn
import getopt
import getpass
import logging
import os
import platform
import re
import subprocess as sub
import sys
import time
import webbrowser

HELP= '''
Connect to a remote DCV Cloud Developer Desktop.
'''

DCV_CONNECT_SCRIPT = "/usr/libexec/dcvcddconnect.sh"
VERSION = "0.2.1"


logging.basicConfig(format='🔥 %(levelname)s : %(message)s ', level=logging.INFO)
log = logging.getLogger('CDD')


class DCVConnectionError(Exception):
    """Error raised with DCV connection fails."""

    pass


SESSION_TOKEN_EXPIRE_SECONDS = 30
GUEST_SESSION_TOKEN_EXPIRE_SECONDS = 120


def _check_command_output(cmd):
    env = os.environ
    if re.match("^MSYS_NT.*", platform.system()):
        # disable path expansion on MSYS/MSYS2
        env['MSYS2_ARG_CONV_EXCL'] = '*'
        env['MSYS_NO_PATHCONV'] = '1'

    return sub.check_output(cmd, shell=True, universal_newlines=True, stderr=sub.STDOUT, env=env).strip()


def error(message, fail_on_error=True):
    """Print an error message and Raise SystemExit exception to the stderr if fail_on_error is true."""
    log.error(message)
    if fail_on_error:
        sys.exit(1)


def check_version(server_version, instance, user, ssh_tool):
    """Compare the script version with the provided version, if it's lower print a warning."""
    server_version_list = server_version.split(".")
    current_version_list = VERSION.split(".")
    server = (server_version_list[0], server_version_list[1], server_version_list[2])
    current = (current_version_list[0], current_version_list[1], current_version_list[2])
    if server > current:
        path = os.path.realpath(__file__)
        remote_path = '/usr/share/dcv/cdd/dcv-cdd.py'
        command = 'pscp' if ssh_tool == 'plink' else 'scp'
        update_command = "{} {}@{}:{} {}\n".format(command, user, instance, remote_path, path)
        log.warning("Client script version is {} but a later version {} is available on the server.\n"
                    "Please update the client script using the following or equivalent command:\n{}"
                    .format(VERSION, server_version, update_command))


def retry(func, func_args, attempts=1, wait=0):
    """
    Call function and re-execute it if it raises an Exception.
    :param func: the function to execute.
    :param func_args: the positional arguments of the function.
    :param attempts: the maximum number of attempts. Default: 1.
    :param wait: delay between attempts. Default: 0.
    :returns: the result of the function.
    """
    while attempts:
        try:
            return func(*func_args)
        except Exception as e:
            attempts -= 1
            if not attempts:
                raise e

            log.info("Error occurred: {0}\nRetrying in {1} seconds...".format(e, wait))
            time.sleep(wait)


def _get_native_default_path():
    try:
        # Check if dcvviewer is in the path
        path = "dcvviewer"
        cmd = path + " --version"
        sub.check_output(cmd, shell=True, stderr=sub.STDOUT)
    except sub.CalledProcessError:
        # Check if it is in a standard location
        local_sys = platform.system()

        if local_sys == "Windows":
            path = "\"C:\\Program Files (x86)\\NICE\\DCV\\Client\\bin\\dcvviewer.exe\""
        elif re.match("^MSYS_NT.*", local_sys):
            # MSYS
            path = "/c/Program\ Files\ \(x86\)/NICE/DCV/Client/bin/dcvviewer.exe"
        elif local_sys == "Linux":
            path = "/usr/bin/dcvviewer"
        elif local_sys == "Darwin":
            # Mac OsX
            path = "/Applications/DCV\\ Viewer.app/Contents/MacOS/dcvviewer"
        else:
            path = None

        if path:
            try:
                cmd = path + " --version"
                sub.check_output(cmd, shell=True, stderr=sub.STDOUT)
            except sub.CalledProcessError:
                path = None

    return path


def dcv_run_command(args):
    """
    Execute cloud developer desktop dcv connect command.
    :param args: options for the command
    """

    # autodetect SSH tool
    ssh_tool = args.ssh_tool
    if ssh_tool == 'auto':
        import psutil

        if 'pageant.exe' in (p.name() for p in psutil.process_iter()) and distutils.spawn.find_executable('plink'):
            ssh_tool = 'plink'
        else:
            ssh_tool = 'ssh'

    if not distutils.spawn.find_executable(ssh_tool):
        error("No {} tool found in PATH, cannot connect to the remote developer desktop".format(ssh_tool), True)
        return

    # Prepare ssh command to execute in the master instance
    ssh_command = ssh_tool + ' -batch' if ssh_tool == 'plink' else ssh_tool

    # connect / create-session / close-session / share
    dcv_cmd = args.command

    # Guest's arguments
    if args.command == "share":
        if args.get_token_guest:
            dcv_cmd += " getTokenGuest " + args.get_token_guest
        elif args.remove_guest:
            dcv_cmd += " removeGuest " + args.remove_guest
        elif args.update_guest:
            dcv_cmd += " updateGuest " + args.update_guest
        elif args.list_guests:
            dcv_cmd += " listGuests"

    # Permissions' alias
    if args.command == "share" and args.mode:
        dcv_cmd += " " + args.mode

    cmd = '{SSH_COMMAND} {USER}@{INSTANCE} "{REMOTE_COMMAND} {DCV_COMMAND}"'.format(
        SSH_COMMAND=ssh_command,
        USER=args.user,
        INSTANCE=args.instance,
        REMOTE_COMMAND=DCV_CONNECT_SCRIPT,
        DCV_COMMAND=dcv_cmd
    )

    # Connection requested (either for the owner or for a guest)
    if args.command == "connect":
        native = False

        if args.native:
            native = True
            native_path = args.path if args.path else _get_native_default_path()
            if not native_path:
                error("No native client found in path.\n" +
                      "Try specifing a path with the -p option or use the web client.")
        elif not args.native and not args.web:
            native_path = _get_native_default_path()
            if native_path:
                native = True
            else:
                log.info("No DCV native client found in default path, using the web client.")

        try:
            res = retry(_retrieve_dcv_from_ssh, func_args=[cmd, args.instance, args.user, ssh_tool, native], attempts=4)
            log.debug("Got the following message through the SSH connection:\n{}".format(res))
        except DCVConnectionError as e:
            error("Error occurred with the SSH connection: {}".format(e), True)

        if native:
            try:
                cmd = "{0} {1} {2}".format(native_path, ' '.join(args.native_params), ' '.join(res.split()))
                log.debug("Launching command: {}".format(cmd))
                sub.check_call(cmd, shell=True, stderr=sub.STDOUT)
            except sub.CalledProcessError as e:
                error("Failed to launch the '{}' client: {}".format(native_path, e.output))
        else: # web client
            try:
                log.debug("Launching webbrowser with URL '{}'".format(res))
                if not webbrowser.open_new(res):
                    raise webbrowser.Error("Unsupported system")
            except webbrowser.Error as e:
                log.warning("Unable to open the web browser: {}".format(e))
                log.info("Please use the following one-time URL in your browser within {} seconds:\n{}"
                         .format(SESSION_TOKEN_EXPIRE_SECONDS, res))

    elif args.command == "share":
        if args.get_token_guest:
            try:
                res = retry(_retrieve_dcv_from_ssh, func_args=[cmd, args.instance, args.user, ssh_tool, False], attempts=4)
            except DCVConnectionError as e:
                error("Something went wrong during DCV connection: {}".format(e))

            # Failed to connect. Print message
            if res.startswith("Message from server:"):
                error(res)
            log.info("Please use the following one-time URL in your browser within {} seconds:\n{}"
                     .format(GUEST_SESSION_TOKEN_EXPIRE_SECONDS, res))

        else:
            # Some other guest operation requested
            try:
                res = retry(_retrieve_dcv_from_ssh, func_args=[cmd, args.instance, args.user, ssh_tool, False], attempts=4)
            except DCVConnectionError as e:
                error("Something went wrong during DCV connection: {}".format(e))
            log.info(res)

    else:
        retry(_execute_dcv_session_command, func_args=[cmd], attempts=4)


def _retrieve_dcv_from_ssh(ssh_cmd, instance, user, ssh_tool, native):
    """
    Connect by ssh to the master instance, prepare DCV session
    and return the DCV session URL or native parameters.
    """
    try:
        log.debug("Launching SSH command: {}".format(ssh_cmd))
        output = _check_command_output(ssh_cmd)
        # At first ssh connection, the ssh command alerts it is adding the host to the known hosts list
        if re.search("Permanently added .* to the list of known hosts.", output):
            output = _check_command_output(ssh_cmd)

        dcv_parameters = re.search(
            r"DcvServerPort=([\d]+) DcvSessionId=([\w]+) DcvSessionToken=([\w-]+) DcvClientScriptVersion=([\d]+[.][\d]+[.][\d]+)", output
        )
        if dcv_parameters:
            dcv_server_port = dcv_parameters.group(1)
            dcv_session_id = dcv_parameters.group(2)
            dcv_session_token = dcv_parameters.group(3)
            server_script_version = dcv_parameters.group(4)
        elif output.startswith("Message from server:"):
            # a message was generated
            return output
        else:
            error(
                "Something went wrong during DCV connection. Please manually execute the command:\n{0}\n".format(ssh_cmd)
            )

    except sub.CalledProcessError as e:
        if "{0}: No such file or directory".format(DCV_CONNECT_SCRIPT) in e.output:
            error(
                "Check if the DCV server is installed on your Cloud Developer Desktop"
            )
        else:
            raise DCVConnectionError(e.output)

    check_version(server_script_version, instance, user, ssh_tool)

    if native:
        return "\"{IP}:{PORT}#{SESSION_ID}\" --auth-token={TOKEN}".format(
            IP=instance,
            PORT=dcv_server_port,
            SESSION_ID=dcv_session_id,
            TOKEN=dcv_session_token
        )

    # else web
    return "https://{IP}:{PORT}?authToken={TOKEN}#{SESSION_ID}".format(
        IP=instance,
        PORT=dcv_server_port,
        TOKEN=dcv_session_token,
        SESSION_ID=dcv_session_id
    )


def _execute_dcv_session_command(ssh_cmd, instance, user, ssh_tool):
    """
    Connect by ssh to the master instance, and executes the related command.
    """
    try:
        output = _check_command_output(ssh_cmd)
        # At first ssh connection, the ssh command alerts it is adding the host to the known hosts list
        if re.search("Permanently added .* to the list of known hosts.", output):
            output = _check_command_output(ssh_cmd)

        dcv_parameters = re.search(
            r"DcvClientScriptVersion=([\d]+[.][\d]+[.][\d]+)", output
        )
        if dcv_parameters:
            server_script_version = dcv_parameters.group(1)
        else:
            error(
                "Something went wrong during DCV connection. Please manually execute the command:\n{0}\n".format(ssh_cmd)
            )

        check_version(server_script_version, instance, user, ssh_tool)

        log.info(output.split("\n")[1])

    except sub.CalledProcessError as e:
        if "{0}: No such file or directory".format(DCV_CONNECT_SCRIPT) in e.output:
            error(
                "Check if the DCV server is installed on your Cloud Developer Desktop"
            )
        else:
            raise DCVConnectionError(e.output)


def _parse_shared_args(parser):
    current_user = getpass.getuser()

    # Mandatory
    parser.add_argument("instance", type=str,
                        help="The instance address where you want to connect")

    # Other optional arguments
    parser.add_argument("-u", "--user", type=str,
                        default=current_user,
                        help="The username for the connection (default: {0})".format(current_user))

    if re.match("^(MSYS_NT|Windows).*", platform.system()):
        ssh_choices = ["ssh", "plink"]
        if re.match("^MSYS_NT.*", platform.system()):
            default_ssh_tool= 'ssh'
        else:
            default_ssh_tool = 'auto'
            ssh_choices += ['auto'] # psutil used to audo-detect cannot be installed on MSYS
        help="Only for Windows, optional - Use it only if you want to specify the SSH tool to be used: " +\
             "use 'plink' if you configured PuTTY with Midway or 'ssh' if you configured OpenSSH with Midway (default: {0})".format(default_ssh_tool)
    else:
        ssh_choices = ["ssh"]
        default_ssh_tool= 'ssh'
        help=argparse.SUPPRESS

    parser.add_argument("--ssh-tool", type=str, choices=ssh_choices, default=default_ssh_tool, help=help)


def _parse_connect_args(parser):
    _parse_shared_args(parser)

    # Optional group - mutual exclusive choice about client
    group_client = parser.add_mutually_exclusive_group()
    group_native = group_client.add_mutually_exclusive_group()
    group_native.add_argument("-n", "--native", action="store_true",
                              help="Use the native client. Search the dcvviewer in path or in the standard path")
    group_native.add_argument("-p", "--path", type=str,
                              metavar="dcvviewer_path",
                              help="Use the native client. Pass the path of client executable dcvviewer")
    group_client.add_argument("-w", "--web", action="store_true",
                              help="Use the web client (calls the default browser)")


def _parse_share_args(parser):
    _parse_shared_args(parser)

    group = parser.add_mutually_exclusive_group()

    group.add_argument("--get-token", type=str, metavar="GUESTNAME", dest="get_token_guest",
                        help="One time URL for session access")
    group.add_argument("--remove", type=str, metavar="GUESTNAME", dest="remove_guest",
                        help="Revoke the permission of a guest")
    group.add_argument("--update", type=str, metavar="GUESTNAME", dest="update_guest",
                        help="Update permissions of a guest")
    group.add_argument("--list", action="store_true", dest="list_guests",
                        help="List all guests with access permissions to session")
    # --mode has "viewer" as default value, this default value is defined in the external authenticator dcvcddextauth.py and not here
    parser.add_argument("--mode", type=str, metavar="PERMISSIONS",
                        help="Guest permissions mode (can be 'viewer' or 'full_access')")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=HELP)
    subparsers = parser.add_subparsers(title="command", help="Command to be executed remotely.\nUse 'dcv-cdd.py COMMAND_NAME --help' for more usage details.", dest="command")

    _parse_connect_args(subparsers.add_parser("connect"))
    _parse_shared_args(subparsers.add_parser("create-session"))
    _parse_shared_args(subparsers.add_parser("close-session"))
    _parse_share_args(subparsers.add_parser("share"))

    # Version
    parser.add_argument("--version", action="version",
                        version="{prog}s {version}".format(prog="%(prog)", version=VERSION))

    parser.add_argument("--debug", "-d", default=False, action="store_true",
                        help="Set the debug level to debug")

    args, unknown = parser.parse_known_args()
    if args.command == "connect" and args.path:
        args.native = True

    if args.debug:
        log.setLevel(logging.DEBUG)

    if unknown:
        if unknown[0] == '--':
            args.native_params = unknown[1:]
        else:
            args.native_params = unknown
    else:
        args.native_params = ['']

    dcv_run_command(args)

# ex:ts=4:et:

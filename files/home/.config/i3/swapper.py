#!/usr/bin/env python3

import i3ipc

# NOTE: for documentation look in: https://github.com/altdesktop/i3ipc-python
# for this to work you need python3 and i3ipc installed on your machine.
# Install it with pip3 install i3ipc.

# Initiate i3ipc library.
i3 = i3ipc.Connection()

# We need tree of containers to find focused and the biggest one.
tree = i3.get_tree()

# Class of currently focused container.
focused = tree.find_focused()

# Check if focused is floating.
# Possible states are: 'auto_on', 'auto_off', 'user_on', 'user_off'
focused_floating_state = 'on' in focused.floating


def find_biggest_container(list_con):
    """Gets list of leaves on current workspace as input and returns
    largest one (by area) that isn't already focused."""

    largest = 0
    largest_container = list_con[0]
    for container in list_con:
        y = container.rect.height
        x = container.rect.width
        # It makes no sense to swap focused with itself
        if x * y >= largest and not container.focused:
            largest = x * y
            largest_container = container

    return largest_container


# Get swappin'
if focused_floating_state:  # If currently focused is floating, do nothing.
    pass
else:
    # List of containers on focused workspace.
    list_con = focused.workspace().leaves()

    # Firstly we need con_id (i3 container id) of the largest container - swapee
    largest_con_id = find_biggest_container(list_con).id
    # and then we swap it.
    i3.command('swap container with con_id {}'.format(largest_con_id))

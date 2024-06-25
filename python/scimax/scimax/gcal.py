"""Scimax interface to Google calender -> org-file

This is used as CLI:
gcal private-calendar-url org-file
"""

from ics import Calendar
import arrow
import requests
import click


@click.command(help='Scimax Google Calendar to org-file')
@click.argument('calendar_url')
@click.argument('org_file')
def main(calendar_url, org_file):
    """
    calendar_url : url to your private gcal basic.ics
    org_file : path to file to write to. if org_file is '-', print the results.
    """
    c = Calendar(requests.get(calendar_url).text)
    now = arrow.utcnow().to('US/Eastern')

    org = ''
    
    for event in sorted(c.events):
        # I don't write old events here, only ones newer than now
        if event.begin.to('US/Eastern') > now:
            begin = event.begin.to('US/Eastern').format('<YYYY-MM-DD ddd HH:mm>')
            end = event.end.to('US/Eastern').format('<YYYY-MM-DD ddd HH:mm>')
            org += f'''** TODO {event.name}
DEADLINE: {begin}--{end}
:PROPERTIES:
:LOCATION: {event.location}
:ATTENDEES: {','.join([attendee.email for attendee in event.attendees])}
:END:
    
{event.description}

'''

    content = f'''#+TITLE: Google Calendar events - generated do no edit
#+filetags: gcal

    [[elisp:(sgcal)][Refresh]]
    
* Events

{org}
'''
    if org_file == '-':
        print(content)
    else:        
        with open(org_file, 'w') as f:
            f.write(content)        

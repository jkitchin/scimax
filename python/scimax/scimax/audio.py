import click
import pathlib
import moviepy.editor as mp
import speech_recognition as sr
import tempfile

@click.command()
@click.argument('avfile')
def a2t(avfile):
    """
    AVFILE is a filename to be read and transcribed to text.
    AIFF is directly converted to text.
    MOV files are extracted to wav then converted to text.
    
    This function prints the text to stdout.
    """
    r = sr.Recognizer()
    
    if pathlib.Path(avfile).suffix == '.aiff':
        
        audio_file = sr.AudioFile(avfile)
        with audio_file as source:
            audio = r.listen(source)
            try:
                text = r.recognize_sphinx(audio)
                print(text)
            except sr.UnknownValueError:
                print("Could not understand audio.")

    elif pathlib.Path(avfile).suffix == '.mov':
        vid = mp.VideoFileClip(avfile)
        (fh, tf) = tempfile.mkstemp(suffix='.wav')
        vid.audio.write_audiofile(tf, logger=None)
        with sr.AudioFile(tf) as source:
            data = r.record(source)

        try:
            print(r.recognize_sphinx(data))        
        except sr.UnknownValueError:
                print("Could not understand audio.")

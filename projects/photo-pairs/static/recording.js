// Introduces the recording of a pair with a "Listen" button, and puts the
// player's own controls in its place once the reader presses it. A bare
// progress bar sitting at the top of the page says nothing about what it
// would play.
//
// The button is added here, rather than in the page itself, so that readers
// without JavaScript are left with a working player.
document.querySelectorAll('.recording').forEach(recording => {
    const audio = recording.querySelector('audio');

    if (!audio) {
        return;
    }

    const button = document.createElement('button');
    button.type = 'button';
    button.className = 'recording-listen';
    button.innerHTML = '<i class="fa-solid fa-play"></i>Listen';

    button.addEventListener('click', () => {
        recording.classList.remove('recording-unplayed');
        button.remove();

        // The recording is started by a click, so it will not be blocked by
        // the browser's autoplay policy. If it fails to start for some other
        // reason, the reader still has the player's own play button.
        audio.play().catch(() => {});

        // Hand the player the keyboard focus the button is giving up.
        audio.focus();
    });

    recording.classList.add('recording-unplayed');
    recording.insertBefore(button, audio);
});

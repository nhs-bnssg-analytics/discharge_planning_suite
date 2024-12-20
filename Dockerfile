FROM ubuntu2204_r422_dpp

RUN apt update && apt -y install cron
RUN mkdir utils

# Copy the scripts
COPY utils/utils.R /root/utils/utils.R
COPY code_main.R /root/code_main.R
COPY code_admits_fcast.R /root/code_admits_fcast.R
COPY code_new_admits.R /root/code_new_admits.R
COPY code_curr_admits.R /root/code_curr_admits.R
COPY code_queue_sim.R /root/code_queue_sim.R
COPY utils/theme.R /root/utils/theme.R
COPY utils/colour_functions.R /root/utils/colour_functions.R

# Copy hello-cron file to the cron.d directory
COPY crontab /etc/cron.d/crontab

# Give execution rights on the cron job
RUN chmod 0644 /etc/cron.d/crontab

# Apply cron job
RUN crontab /etc/cron.d/crontab

# Run the command on container startup
CMD printenv > /etc/environment && cron -f
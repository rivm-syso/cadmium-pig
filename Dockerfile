FROM image-registry.openshift-image-registry.svc:5000/sscc-baseimages/rshiny:v4.4.3
COPY model /opt/R-apps/cadmium-pig
RUN echo 'local({options(shiny.port = 3838, shiny.host = "0.0.0.0")})' >> $R_HOME/etc/Rprofile.site
EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('/opt/R-apps/cadmium-pig')"]
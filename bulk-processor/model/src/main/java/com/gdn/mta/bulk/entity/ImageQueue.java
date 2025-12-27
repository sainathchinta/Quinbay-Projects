package com.gdn.mta.bulk.entity;

import java.io.Serializable;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ImageQueue implements Serializable {

  private static final long serialVersionUID = 5729158354600440893L;
  private String filename;
  private String file;

  public ImageQueue() {
  }

  public ImageQueue(String filename, String file) {
    super();
    this.filename = filename;
    this.file = file;
  }

  public String getFile() {
    return file;
  }

  public String getFilename() {
    return filename;
  }

  public void setFile(String file) {
    this.file = file;
  }

  public void setFilename(String filename) {
    this.filename = filename;
  }

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("ImageQueue [filename=").append(filename).append(", getFile()=")
        .append(getFile()).append(", getFilename()=").append(getFilename()).append("]");
    return builder.toString();
  }

}
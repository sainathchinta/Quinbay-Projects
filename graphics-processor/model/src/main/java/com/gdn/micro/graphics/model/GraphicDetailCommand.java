package com.gdn.micro.graphics.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.Setter;

import java.io.File;
import java.io.Serializable;

@JsonIgnoreProperties(ignoreUnknown = true)
public class GraphicDetailCommand implements Serializable {

  private static final long serialVersionUID = -8635667380039642097L;

  private final File temporaryFile;
  @Setter
  private String sourcePath;
  private final String destinationPath;
  private final CustomGraphicsSettings customGraphicsSettings;
  private final String prefixPath;
  private final boolean uploadToGcs;

  private String sourceGcsPath;

  public GraphicDetailCommand(File temporaryFile, String sourcePath, String destinationPath,
      CustomGraphicsSettings customGraphicsSettings, String prefixPath, boolean uploadToGcs) {
    this.temporaryFile = temporaryFile;
    this.sourcePath = sourcePath;
    this.destinationPath = destinationPath;
    this.customGraphicsSettings = customGraphicsSettings;
    this.prefixPath = prefixPath;
    this.uploadToGcs = uploadToGcs;
  }

  public CustomGraphicsSettings getCustomGraphicsSettings() {
    return customGraphicsSettings;
  }

  public String getDestinationPath() {
    return destinationPath;
  }

  public String getPrefixPath() {
    return prefixPath;
  }

  public String getSourcePath() {
    return sourcePath;
  }

  public File getTemporaryFile() {
    return temporaryFile;
  }

  public boolean isUploadToGcs() {
    return uploadToGcs;
  }

  public String getSourceGcsPath() {
    return sourceGcsPath;
  }

  public void setSourceGcsPath(String sourceGcsPath) {
    this.sourceGcsPath = sourceGcsPath;
  }
}

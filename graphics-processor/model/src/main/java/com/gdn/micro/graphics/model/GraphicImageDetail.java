package com.gdn.micro.graphics.model;

import java.io.Serializable;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Created by Vishal on 10/06/18.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@AllArgsConstructor
@NoArgsConstructor
@Data
public class GraphicImageDetail implements Serializable {

  private long serialVersionUID = 5919165394193279296L;
  private String groupCode;
  private String hashCode;
  private String sourcePath;
  private String destinationPath;
  private CustomGraphicsSettings customGraphicsSettings;
  private String prefixPath;
  private boolean commonImage;

  public GraphicImageDetail(String hashCode, String sourcePath, String destinationPath,
      CustomGraphicsSettings customGraphicsSettings, String prefixPath, String groupCode) {
    this.groupCode = groupCode;
    this.hashCode = hashCode;
    this.sourcePath = sourcePath;
    this.destinationPath = destinationPath;
    this.customGraphicsSettings = customGraphicsSettings;
    this.prefixPath = prefixPath;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("groupCode", groupCode).append("hashCode", hashCode)
        .append("sourcePath", sourcePath).append("destinationPath", destinationPath)
        .append("customGraphicsSettings", customGraphicsSettings).append("prefixPath", prefixPath)
        .toString();
  }
}

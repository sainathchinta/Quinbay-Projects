package com.gdn.mta.product.entity;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class VideoAddEditRequest implements Serializable {
  private static final long serialVersionUID = -6353970482783592901L;

  private String videoUrl;
  private String videoId;
  private String videoName;
  private String coverImagePath;
}

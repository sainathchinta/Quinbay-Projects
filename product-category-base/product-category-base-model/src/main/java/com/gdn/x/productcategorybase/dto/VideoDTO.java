package com.gdn.x.productcategorybase.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serial;
import java.io.Serializable;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class VideoDTO implements Serializable {
  @Serial
  private static final long serialVersionUID = 355810249541640088L;
  private String videoId;
  private String finalUrl;
  private String videoName;
  private String coverImagePath;
  private String sourceUrl;
}

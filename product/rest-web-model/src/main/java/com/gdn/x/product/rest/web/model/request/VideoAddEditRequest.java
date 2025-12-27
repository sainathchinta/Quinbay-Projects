package com.gdn.x.product.rest.web.model.request;

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
  private static final long serialVersionUID = 3662508080640988032L;
  private String videoUrl;
  private String videoId;
  private String videoName;
  private String coverImagePath;
}

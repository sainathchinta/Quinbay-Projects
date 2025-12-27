package com.gdn.mta.bulk.models;

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
  private static final long serialVersionUID = -4388152409995548240L;
  private String videoUrl;
  private String videoId;
  private String videoName;
  private String coverImagePath;
}

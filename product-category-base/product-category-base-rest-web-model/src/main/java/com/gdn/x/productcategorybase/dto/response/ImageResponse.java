package com.gdn.x.productcategorybase.dto.response;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTOResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ImageResponse extends BaseDTOResponse implements Serializable {
  private static final long serialVersionUID = -538856808160968199L;

  private boolean mainImage;
  private String locationPath;
  private Integer sequence;
  private boolean markForDelete;
  private boolean active;
  private boolean edited;
  private boolean revised;
  private boolean commonImage;
  private Boolean originalImage;
}

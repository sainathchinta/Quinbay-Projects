package com.gdn.partners.pcu.external.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductL3CommonImageWebResponse extends BaseResponse{

  private static final long serialVersionUID = -7391255382969622954L;
  private Boolean mainImages;
  private Integer sequence;
  private String locationPath;
  private Boolean activeLocation;
  private Boolean markForDelete;
  private boolean commonImage = true;
}

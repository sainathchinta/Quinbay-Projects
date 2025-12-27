package com.gdn.partners.pcu.external.web.model.request;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class CopyImageEditWebRequest {
  private boolean mainImage;
  private boolean copy;
  private boolean markForDelete;
}

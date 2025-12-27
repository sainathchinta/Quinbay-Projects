package com.gdn.partners.pcu.internal.web.model.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class PDTStateUpdateWebRequest {

  private String state;
  private Boolean revised;
  private Boolean edited;
  private String reviewType;
  private Boolean markForDelete;

}
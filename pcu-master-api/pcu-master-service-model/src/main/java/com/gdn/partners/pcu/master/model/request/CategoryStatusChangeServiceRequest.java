package com.gdn.partners.pcu.master.model.request;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class CategoryStatusChangeServiceRequest extends CategoryInfoUpdateServiceRequest {
  private boolean activated;
  private String catalogType;
}

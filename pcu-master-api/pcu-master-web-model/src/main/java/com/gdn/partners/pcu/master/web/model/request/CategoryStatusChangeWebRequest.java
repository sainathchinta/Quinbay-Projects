package com.gdn.partners.pcu.master.web.model.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class CategoryStatusChangeWebRequest extends CategoryInfoUpdateWebRequest {
  private String catalogType;
  private boolean isActivated;
}

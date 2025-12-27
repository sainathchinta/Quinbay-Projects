package com.gdn.x.productcategorybase.dto.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serial;
import java.io.Serializable;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class BrandAuthorisationWipListRequest implements Serializable {
  @Serial
  private static final long serialVersionUID = -7683073609797451242L;
  private String keyword;
  private String status;
  private String sellerCode;
  private String tabName;
}

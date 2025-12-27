package com.gdn.partners.pcu.external.web.model.request;

import java.util.Date;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Created by parvej on 28/05/2021 AD.
 */

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class PreOrderWebRequest {

  private Boolean isPreOrder;
  private String preOrderType;
  private Integer preOrderValue;
  private Date preOrderDate;
}

package com.gdn.partners.pcu.external.web.model.request;


import java.util.ArrayList;
import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class QRProductWebRequest {

  private String productSku;
  private String productName;
  private List<QRItemWebRequest> items = new ArrayList();
}

package com.gdn.partners.pcu.internal.client.model.request;

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
  private static final long serialVersionUID = -4521977269162472081L;
  private String keyword;
  private String status;
  private String sellerCode;
  private String tabName;
}

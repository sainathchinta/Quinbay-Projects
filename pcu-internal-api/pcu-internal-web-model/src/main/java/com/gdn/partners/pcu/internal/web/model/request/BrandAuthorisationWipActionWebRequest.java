package com.gdn.partners.pcu.internal.web.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.Date;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class BrandAuthorisationWipActionWebRequest implements Serializable {
  private static final long serialVersionUID = 1193590673892252836L;
  private String sellerCode;
  private String brandCode;
  private Date authStartDate;
  private Date authExpireDate;
  private String reason;
  private String action;
}

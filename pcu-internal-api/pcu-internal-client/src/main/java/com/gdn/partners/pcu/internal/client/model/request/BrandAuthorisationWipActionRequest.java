package com.gdn.partners.pcu.internal.client.model.request;

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
public class BrandAuthorisationWipActionRequest implements Serializable {
  private static final long serialVersionUID = -3350202174654117430L;
  private String sellerCode;
  private String brandCode;
  private Date authStartDate;
  private Date authExpireDate;
  private String reason;
  private String action;
}

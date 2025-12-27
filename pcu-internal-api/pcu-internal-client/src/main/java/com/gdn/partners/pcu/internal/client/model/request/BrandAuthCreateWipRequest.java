package com.gdn.partners.pcu.internal.client.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.Date;
import java.util.List;
import java.util.ArrayList;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class BrandAuthCreateWipRequest implements Serializable {
  private static final long serialVersionUID = -3143764575169191698L;
  private String brandName;
  private String brandCode;
  private String sellerCode;
  private Date authStartDate;
  private Date authExpireDate;
  private List<String> documentLinks = new ArrayList<>();
  private String iprRegistrationNumber;
}

package com.gdn.partners.pcu.internal.client.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class BrandAuthUpdateRequest implements Serializable {
  private static final long serialVersionUID = -5544356747840430904L;
  private String sellerCode;
  private String brandCode;
  private String brandName;
  private String authorisationStatus;
  private Date authStartDate;
  private Date authExpireDate;
  private List<String> documentLinks = new ArrayList<>();
  private String iprRegistrationNumber;
}

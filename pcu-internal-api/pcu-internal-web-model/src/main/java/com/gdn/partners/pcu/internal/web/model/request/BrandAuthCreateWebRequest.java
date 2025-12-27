package com.gdn.partners.pcu.internal.web.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class BrandAuthCreateWebRequest implements Serializable {
  private static final long serialVersionUID = 7652911430592054779L;
  private String brandName;
  private String brandCode;
  private String sellerCode;
  private String authorisationStatus;
  private Date authStartDate;
  private Date authExpireDate;
  private List<String> documentLinks;
}

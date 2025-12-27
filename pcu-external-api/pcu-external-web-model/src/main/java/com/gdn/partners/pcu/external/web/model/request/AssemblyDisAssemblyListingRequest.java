package com.gdn.partners.pcu.external.web.model.request;

import java.util.Date;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;


@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString
@Builder
public class AssemblyDisAssemblyListingRequest {
  private String merchantCode;
  private String type;
  private String status;
  private Date startDate;
  private Date endDate;
  private String keyword;
}


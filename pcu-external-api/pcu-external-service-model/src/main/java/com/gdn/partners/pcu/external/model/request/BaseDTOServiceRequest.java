package com.gdn.partners.pcu.external.model.request;

import java.util.Date;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Created by govind on 13/12/2018 AD.
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class BaseDTOServiceRequest {

  private String storeId;
  private String requestId;
  private String username;
  private Date createdDate;
}

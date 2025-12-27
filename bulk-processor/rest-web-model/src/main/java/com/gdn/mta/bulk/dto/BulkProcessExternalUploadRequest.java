package com.gdn.mta.bulk.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

@EqualsAndHashCode(callSuper = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkProcessExternalUploadRequest extends GdnBaseDomainEventModel
  implements Serializable {

  private String storeId;
  private String username;
  private String requestId;
  private BulkProcessType bulkProcessType;
  private String zipFileName;
  private Map<String, String> files = new HashMap<>();
  private String bulkProcessCode;
  private String businessPartnerCode;
  private String pickupPointCode;
  private String onlyExternalUser;
}

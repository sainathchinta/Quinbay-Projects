package com.gdn.partners.pcu.internal.service.impl.event.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@EqualsAndHashCode(callSuper = false)
@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkReviewUploadModel extends GdnBaseDomainEventModel implements Serializable {
  private static final long serialVersionUID = -5286088661795905343L;
  private String storeId;
  private String requestId;
  private String createdBy;
  private String bulkProcessType;
  private String bulkProcessCode;
  private String filePath;
  private String vendorCode;
}
package com.gdn.mta.bulk.dto;

import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import com.gdn.mta.bulk.dto.product.DownloadQRCodeRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class QRCodeExcelQueue extends GdnBaseDomainEventModel {

  private String storeId;
  private String bulkProcessCode;
  private String filename;
  private DownloadQRCodeRequest downloadQRCodeRequest;
}

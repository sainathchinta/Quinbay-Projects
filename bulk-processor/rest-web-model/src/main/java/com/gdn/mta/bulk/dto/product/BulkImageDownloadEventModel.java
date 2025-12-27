package com.gdn.mta.bulk.dto.product;

import java.util.List;

import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class BulkImageDownloadEventModel extends GdnBaseDomainEventModel {
  private String bulkProcessCode;
  private List<String> imageDownloadList;
}

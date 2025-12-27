package com.gdn.mta.bulk.models.download.responsedata;

import java.util.List;

import org.apache.commons.lang3.builder.ToStringBuilder;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;


@Data
@AllArgsConstructor
@EqualsAndHashCode(callSuper = true)
public class BulkProductLiteResponse extends BulkDataResponse {
  private List<List<String>> productContentList;
  private boolean partialDownload;


  @Override
  public String toString() {
    return new ToStringBuilder(this).append("productContentList", productContentList)
        .toString();
  }
}

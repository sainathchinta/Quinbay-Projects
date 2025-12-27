package com.gdn.mta.bulk.models.download;

import org.apache.commons.lang3.builder.ToStringBuilder;

import java.util.List;
import java.util.Map;

/**
 * Created by keshashah on 25/10/16.
 */
public class BulkCsvModel {
  private List<String> headerList;
  private Map<String, String> headerToFieldMap;

  public BulkCsvModel(List<String> headerList, Map<String, String> headerToFieldMap) {
    this.headerList = headerList;
    this.headerToFieldMap = headerToFieldMap;
  }

  public List<String> getHeaderList() {
    return headerList;
  }

  public Map<String, String> getHeaderToFieldMap() {
    return headerToFieldMap;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("headerList", headerList)
        .append("headerToFieldMap", headerToFieldMap).toString();
  }
}

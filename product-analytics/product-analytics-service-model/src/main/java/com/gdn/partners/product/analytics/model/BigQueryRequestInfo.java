package com.gdn.partners.product.analytics.model;

import com.google.cloud.bigquery.TableId;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class BigQueryRequestInfo {
  private String query;
  private String localFileName;
  private String extension;
  private TableId destinationTableId;
  private String resultDataSet;
  private String gcsFilePrefix;
  private String bucketName;
  private String bucketUri;
}
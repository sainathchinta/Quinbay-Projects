package com.gdn.partners.product.analytics.properties;

import org.springframework.boot.context.properties.ConfigurationProperties;

import lombok.Data;

@Data
@ConfigurationProperties(value = "gcp")
public class GCPProperties {

  private String serviceAccountCredentials;

  private String resultDataSet;

  private String resultTablePrefix;

  private String resultGoogleCloudBucketName;

  private String resultFileNamePrefix;

  private int bulkUpdateBatchSize = 1000;

  private int numberOfRecordsPerFile = 1000;

  private String queryProjectId;

  private boolean downloadFileManually = false;

  private String resultFileNameLocal;

  private String projectId;

  private String storageProjectId;

  private String sellerSpecificResultFileNameLocal;

  private String sellerSpecificResultFileNamePrefix;

  private String sellerSpecificResultDataSet;

  private String sellerSpecificResultTablePrefix;
  private String autoApprovedResultFileNameLocal;
  private String autoApprovedResultFileNamePrefix;
  private String autoApprovedResultDataSet;
  private String autoApprovedResultTablePrefix;
  private String genevaProjectId;

  //ipr products properties
  private String datascienceProjectId;
  private String iprProductsResultTablePrefix;
  private String iprProductsResultDataSet;
  private String iprProductsResultFileNamePrefix;
  private String iprProductsResultFileNameLocal;
  private String iprProductFetchTable;
  private String iprBigQueryProjectId;

  //product attribute extractions properties
  private String productAttributeExtractionsBQTable;
  private String prdProductBusinessPartnerBQTable;
  private String productAttributeExtractionsResultTablePrefix;
  private String productAttributeExtractionsResultDataSet;
  private String productAttributeExtractionsResultFileNamePrefix;
  private String productAttributeExtractionsResultFileNameLocal;

  //image deletion properties
  private String imageServiceAccountCredentials;
  private String sourceImageBucketName;
  private String finalImageBucketName;
  private String finalImagePrefix;
  private String sourceImagePrefix;
  private String catalogImagePathPrefix;

  //seller analytics properties
  private String sellerAnalyticsResultFileNameLocal;

  private String sellerAnalyticsResultFileNamePrefix;

  private String sellerAnalyticsResultDataSet;

  private String sellerAnalyticsResultTablePrefix;

  //product optimisation properties
  private String productOptimisationDsTable;
  private String productOptimisationResultTablePrefix;
  private String productOptimisationResultDataSet;
  private String productOptimisationResultFileNamePrefix;
  private String productOptimisationResultFileNameLocal;
}

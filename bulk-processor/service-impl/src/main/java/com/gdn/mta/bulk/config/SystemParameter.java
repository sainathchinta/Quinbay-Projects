package com.gdn.mta.bulk.config;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.stereotype.Component;

import lombok.Data;

@Component
public class SystemParameter {

  @Value("${mta.directory.image.source}")
  public String mtaImageSource;

  @Value("${master.data.bulk.update.batch.size}")
  public int masterDataBulkUpdateBatchSize;

  @Value("${vendor.product.assignee.bulk.update.batch.size}")
  public int vendorProductAssigneeBulkUpdateBatchSize;

  @Value("${mtaapi.tmp.directory.image}")
  private String mtaApiTmpImage;

  @Value("${product.image.max.size.byte}")
  private int imageMaxSize;

  @Value("${product.suspension.bulk.upload.batch.size}")
  public int productSuspensionBulkUploadBatchSize;

  @Value("${download.image.url.read.timeout}")
  private int downloadImageUrlReadTimeout;
  @Value("${download.image.url.connect.timeout}")
  private int downloadImageUrlConnectTimeout;

  @Value("${configuration.bulk.upload.batch.size}")
  public int configurationBulkUploadBatchSize;

  public int getImageMaxSize() {
    return imageMaxSize;
  }

  public void setImageMaxSize(int imageMaxSize) {
    this.imageMaxSize = imageMaxSize;
  }

  public String getMtaApiTmpImage() {
    return mtaApiTmpImage;
  }

  public void setMtaApiTmpImage(String mtaApiTmpImage) {
    this.mtaApiTmpImage = mtaApiTmpImage;
  }

  public String getMtaImageSource() {
    return mtaImageSource;
  }

  public void setMtaImageSource(String mtaImageSource) {
    this.mtaImageSource = mtaImageSource;
  }

  public int getMasterDataBulkUpdateBatchSize() {
    return masterDataBulkUpdateBatchSize;
  }

  public void setMasterDataBulkUpdateBatchSize(int masterDataBulkUpdateBatchSize) {
    this.masterDataBulkUpdateBatchSize = masterDataBulkUpdateBatchSize;
  }

  public int getDownloadImageUrlReadTimeout() {
    return downloadImageUrlReadTimeout;
  }

  public void setDownloadImageUrlReadTimeout(int downloadImageUrlReadTimeout) {
    this.downloadImageUrlReadTimeout = downloadImageUrlReadTimeout;
  }

  public int getDownloadImageUrlConnectTimeout() {
    return downloadImageUrlConnectTimeout;
  }

  public void setDownloadImageUrlConnectTimeout(int downloadImageUrlConnectTimeout) {
    this.downloadImageUrlConnectTimeout = downloadImageUrlConnectTimeout;
  }

  public void setProductSuspensionBulkUploadBatchSize(int productSuspensionBulkUploadBatchSize){
    this.productSuspensionBulkUploadBatchSize = productSuspensionBulkUploadBatchSize;
  }

  public int getProductSuspensionBulkUploadBatchSize(){
    return this.productSuspensionBulkUploadBatchSize;
  }

  public int getConfigurationBulkUploadBatchSize() {
    return configurationBulkUploadBatchSize;
  }

  public void setConfigurationBulkUploadBatchSize(int configurationBulkUploadBatchSize) {
    this.configurationBulkUploadBatchSize = configurationBulkUploadBatchSize;
  }

  public int getVendorProductAssigneeBulkUpdateBatchSize() {
    return vendorProductAssigneeBulkUpdateBatchSize;
  }

  public void setVendorProductAssigneeBulkUpdateBatchSize(int vendorProductAssigneeBulkUpdateBatchSize) {
    this.vendorProductAssigneeBulkUpdateBatchSize = vendorProductAssigneeBulkUpdateBatchSize;
  }
}

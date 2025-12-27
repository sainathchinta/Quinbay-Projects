package com.gdn.mta.bulk.models.download.responsedata;

import org.apache.commons.lang3.builder.ToStringBuilder;

import java.util.List;
import java.util.Map;

/**
 * Created by keshashah on 04/11/16.
 */
public class BulkProductResponse extends BulkDataResponse {
  private List<List<String>> productContentList;
  private boolean businessPartnerO2O;
  private Map<String, String> exceptionMap;
  private Map<String, Boolean> privilegedMap;
  private List<PickupPointModel> pickupPoint;
  private boolean partialDownload;

  public BulkProductResponse(List<List<String>> productContentList, boolean businessPartnerO2O,
      Map<String, Boolean> privilegedMap, Map<String, String> exceptionMap) {
    this.productContentList = productContentList;
    this.businessPartnerO2O = businessPartnerO2O;
    this.privilegedMap = privilegedMap;
    this.exceptionMap = exceptionMap;
  }

  public BulkProductResponse(List<List<String>> productContentList, boolean businessPartnerO2O,
      Map<String, Boolean> privilegedMap, List<PickupPointModel> pickupPoint,Map<String, String> exceptionMap, boolean partialDownload) {
    this.productContentList = productContentList;
    this.businessPartnerO2O = businessPartnerO2O;
    this.privilegedMap = privilegedMap;
    this.pickupPoint = pickupPoint;
    this.exceptionMap = exceptionMap;
    this.partialDownload = partialDownload;
  }

  public List<List<String>> getProductContentList() {
    return productContentList;
  }

  public boolean isBusinessPartnerO2O() {
    return businessPartnerO2O;
  }

  public Map<String, Boolean> getPrivilegedMap() {
    return privilegedMap;
  }

  public List<PickupPointModel> getPickupPoint() {
    return pickupPoint;
  }

  public void setPickupPoint(List<PickupPointModel> pickupPoint) {
    this.pickupPoint = pickupPoint;
  }

  public Map<String, String> getExceptionMap() {
    return exceptionMap;
  }

  public boolean isPartialDownload() {
    return partialDownload;
  }

  public void setPartialDownload(boolean partialDownload) {
    this.partialDownload = partialDownload;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("productContentList", productContentList)
        .append("businessPartnerO2O", businessPartnerO2O).append("privilegedMap", privilegedMap)
        .append("pickupPoint", pickupPoint).append("exceptionMap", exceptionMap).toString();
  }
}

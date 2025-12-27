package com.gdn.mta.bulk.models.download.responsedata;

import java.io.Serializable;
import java.util.Arrays;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.response.MasterProductResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkMasterProductResponse extends BulkDataResponse implements Serializable {
  private static final long serialVersionUID = -4189645376666866759L;
  private static final String DG_LEVEL1 = "0";
  private static final String DG_LEVEL2 = "1";
  private static final String DG_LEVEL3 = "2";

  List<MasterProductResponse> responseList;
  List<String> dGLevels;

  public BulkMasterProductResponse(List<MasterProductResponse> responseList) {
    this.responseList = responseList;
  }

  public BulkMasterProductResponse() {
    this.dGLevels = Arrays.asList(DG_LEVEL1, DG_LEVEL2, DG_LEVEL3);
  }

  public List<MasterProductResponse> getResponseList() {
    return responseList;
  }

  public void setResponseList(List<MasterProductResponse> responseList) {
    this.responseList = responseList;
  }

  public List<String> getdGLevels() {
    return dGLevels;
  }

  public void setdGLevels(List<String> dGLevels) {
    this.dGLevels = dGLevels;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("BulkMasterProductResponse{");
    sb.append("responseList=").append(responseList);
    sb.append('}');
    return sb.toString();
  }
}

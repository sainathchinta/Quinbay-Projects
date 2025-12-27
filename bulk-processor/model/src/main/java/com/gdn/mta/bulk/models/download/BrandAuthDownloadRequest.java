package com.gdn.mta.bulk.models.download;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class BrandAuthDownloadRequest extends BulkDownloadRequest implements Serializable {
  private static final long serialVersionUID = 269665370894299719L;
  private String sellerCode;
  private String brandName;
  private String status;

  @Override
  public String toString() {
    return "BrandAuthDownloadRequest{" + "sellerCode='" + sellerCode + '\'' + ", brandName='" + brandName + '\''
        + ", status='" + status + '\'' + super.toString() + '}';
  }
}

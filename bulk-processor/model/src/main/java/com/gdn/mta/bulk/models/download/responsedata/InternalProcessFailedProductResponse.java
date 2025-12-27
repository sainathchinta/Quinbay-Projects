package com.gdn.mta.bulk.models.download.responsedata;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.mta.bulk.models.InternalBulkProcessFailedData;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class InternalProcessFailedProductResponse extends BulkDataResponse implements Serializable {
  private static final long serialVersionUID = -6954382925995390583L;
  private List<InternalBulkProcessFailedData> internalBulkProcessFailedData = new ArrayList<>();

  @Override
  public String toString() {
    return "InternalProcessFailedProductResponse{" + "storeCopyFailedProducts=" + internalBulkProcessFailedData + '}';
  }
}

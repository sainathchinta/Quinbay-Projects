package com.gdn.x.productcategorybase.dto.brand;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.Date;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class BrandSummaryRequest implements Serializable {

  private static final long serialVersionUID = -8763786088566342030L;
  private String brandName;
  private boolean markForDelete;
  private Date updatedDate;
  private String sortedBy;
  private String sortDirection;
}

package com.gdn.x.productcategorybase.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.domain.Pageable;

import java.util.Date;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class BrandSummaryFilterDTO {

  private String brandName;
  private boolean markForDelete;
  private Date updatedDate;
  private String sortedBy;
  private String sortDirection;
  private Pageable pageable;
}

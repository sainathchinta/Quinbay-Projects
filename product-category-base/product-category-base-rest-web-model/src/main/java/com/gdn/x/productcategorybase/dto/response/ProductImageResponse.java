package com.gdn.x.productcategorybase.dto.response;

import java.io.Serializable;

import com.gdn.x.productcategorybase.dto.BaseDTOResponse;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ProductImageResponse extends BaseDTOResponse implements Serializable {
  private static final long serialVersionUID = -9004673817279389895L;
  
  private boolean mainImage;
  private String locationPath;
  private String productId;
  private String productCode;
  private int sequence;
  private boolean active;
  private Boolean originalImage;
}

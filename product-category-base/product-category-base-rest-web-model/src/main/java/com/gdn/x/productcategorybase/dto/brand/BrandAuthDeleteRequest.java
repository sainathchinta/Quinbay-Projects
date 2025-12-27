package com.gdn.x.productcategorybase.dto.brand;


import jakarta.validation.constraints.NotEmpty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@ToString
public class BrandAuthDeleteRequest {

  @NotEmpty(message = "Seller code cannot be empty")
  private String sellerCode;

  @NotEmpty(message = "Brand code cannot be empty")
  private String brandCode;

  private String id;

  private String status;

}

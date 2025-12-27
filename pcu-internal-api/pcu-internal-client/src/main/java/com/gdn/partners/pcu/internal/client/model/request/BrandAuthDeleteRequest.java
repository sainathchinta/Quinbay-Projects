package com.gdn.partners.pcu.internal.client.model.request;

import javax.validation.constraints.NotEmpty;

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

  @NotEmpty(message = "Id cannot be empty")
  private String id;

  @NotEmpty(message = "Status cannot be empty")
  private String status;

}
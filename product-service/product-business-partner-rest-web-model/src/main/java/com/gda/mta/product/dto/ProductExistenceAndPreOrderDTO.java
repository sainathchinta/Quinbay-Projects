package com.gda.mta.product.dto;
import com.gdn.x.product.rest.web.model.dto.PreOrderDTO;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ProductExistenceAndPreOrderDTO {
  private boolean productExistInXproduct;
  private PreOrderDTO preOrder;
}

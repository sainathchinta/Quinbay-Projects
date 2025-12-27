package com.gdn.aggregate.platform.module.product.listener.model.raw;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serial;
import java.io.Serializable;
import java.util.List;

@Builder
@Data
@AllArgsConstructor
@NoArgsConstructor
public class UpdateSearchEventWarehouseDetails implements Serializable {
  @Serial
  private static final long serialVersionUID = -1234567890123456789L;

  private List<String> nonOosWarehouseCodes;
  private List<String> oosWarehouseCodes;
}

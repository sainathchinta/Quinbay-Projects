package com.gdn.partners.pcu.internal.web.model.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serial;
import java.io.Serializable;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class RejectProductRequest implements Serializable {
  @Serial
  private static final long serialVersionUID = 4150327294041068221L;
  private String productCode;
  private String merchantCommissionType;
}

package com.gda.mta.product.dto.response;

import java.io.Serializable;

import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class ProductCountResponse extends BaseResponse implements Serializable {
  private static final long serialVersionUID = 3343745722042446957L;
  private Long productCount;
}

package com.gdn.x.productcategorybase.outbound.matrix;

import com.gdn.x.productcategorybase.outbound.model.MatrixAttributeExtractionRequest;
import com.gdn.x.productcategorybase.outbound.model.MatrixAttributeExtractionResponse;
import com.gdn.x.productcategorybase.outbound.model.SingleBaseResponse;

public interface MatrixOutbound {

  /**
   * extract product attributes using text details from matrix system
   * @param matrixAttributeExtractionRequest
   * @return
   */
  SingleBaseResponse<MatrixAttributeExtractionResponse> extractProductAttributesByTextDetails(
      MatrixAttributeExtractionRequest matrixAttributeExtractionRequest);

}

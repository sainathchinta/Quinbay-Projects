package com.gdn.x.productcategorybase.outbound.matrix;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.stereotype.Service;

import com.gdn.x.productcategorybase.outbound.model.MatrixAttributeExtractionRequest;
import com.gdn.x.productcategorybase.outbound.model.MatrixAttributeExtractionResponse;
import com.gdn.x.productcategorybase.outbound.matrix.client.MatrixFeign;
import com.gdn.x.productcategorybase.outbound.model.SingleBaseResponse;
import lombok.extern.slf4j.Slf4j;


@Service
@Slf4j
public class MatrixOutboundImpl implements MatrixOutbound {

  @Autowired
  private MatrixFeign matrixFeign;

  @Override
  public SingleBaseResponse<MatrixAttributeExtractionResponse> extractProductAttributesByTextDetails(
      MatrixAttributeExtractionRequest matrixAttributeExtractionRequest) {
    SingleBaseResponse<MatrixAttributeExtractionResponse> response =
        matrixFeign.extractProductAttributesByTextDetails(matrixAttributeExtractionRequest);
    return response;
  }
}

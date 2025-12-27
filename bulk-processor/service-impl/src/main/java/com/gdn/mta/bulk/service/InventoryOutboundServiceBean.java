package com.gdn.mta.bulk.service;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.mta.bulk.dto.inventory.InventoryDetailInfoRequestDTO;
import com.gdn.mta.bulk.dto.inventory.InventoryDetailInfoResponseDTO;
import com.gdn.mta.bulk.feignConfig.InventoryFeign;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.partners.pbp.dto.common.ListRequestDTO;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Slf4j
@Service
public class InventoryOutboundServiceBean implements InventoryOutboundService {

  @Autowired
  private InventoryFeign inventoryFeign;

  @Override
  public List<InventoryDetailInfoResponseDTO> findDetailByWebMerchantCodeAndWebItemSku(
    ListRequestDTO<InventoryDetailInfoRequestDTO> inventoryDetailInfoRequestDTO) {
    GdnRestListResponse<InventoryDetailInfoResponseDTO> response =
      inventoryFeign.findDetailByWebMerchantCodeAndWebItemSku(Constant.STORE_ID,
        Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID, Constant.USER_NAME,
        inventoryDetailInfoRequestDTO);
    if (!response.isSuccess()) {
      log.error("Error on fetching Inventory details for request {}, error : {}",
        inventoryDetailInfoRequestDTO, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getContent();
  }
}

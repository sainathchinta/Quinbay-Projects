package com.gdn.mta.bulk.service;


import com.gdn.mta.bulk.dto.inventory.InventoryDetailInfoRequestDTO;
import com.gdn.mta.bulk.dto.inventory.InventoryDetailInfoResponseDTO;
import com.gdn.partners.pbp.dto.common.ListRequestDTO;

import java.util.List;

public interface InventoryOutboundService {

  List<InventoryDetailInfoResponseDTO> findDetailByWebMerchantCodeAndWebItemSku(
    ListRequestDTO<InventoryDetailInfoRequestDTO> inventoryDetailInfoRequestDTO);
}

package com.gdn.mta.product.repository;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.partners.pbp.model.vo.ProductLv1IdxLv3IdVO;

public interface ProductBusinessPartnerCustomRepository {
  
  List<ProductLv1IdxLv3IdVO> findProductLv1IdxLv3IdVO(String storeCode, String merchantSKu, 
      List<String> productIds);

  Page<ProductBusinessPartner> findRejectedProductsByStoreIdAndBusinessPartnerIdAndProductName(String storeId,
      String businessPartnerId, String productName, Pageable pageable, String orderBy, String sortBy);
}

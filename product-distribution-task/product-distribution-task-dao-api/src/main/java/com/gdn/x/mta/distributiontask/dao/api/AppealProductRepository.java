package com.gdn.x.mta.distributiontask.dao.api;

import com.gdn.x.mta.distributiontask.model.AppealedProduct;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;


@Repository
public interface AppealProductRepository extends JpaRepository<AppealedProduct, String> {
  AppealedProduct findByProductCode(String productCode);
}

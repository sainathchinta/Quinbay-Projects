package com.gdn.partners.product.orchestrator.repository.data.product.level1;

import com.gdn.partners.product.orchestrator.entity.product.level1.ProductLevel1;
import org.springframework.data.jpa.repository.JpaRepository;

public interface ProductLevel1Repository
    extends JpaRepository<ProductLevel1, String>, ProductLevel1CustomRepository {
}

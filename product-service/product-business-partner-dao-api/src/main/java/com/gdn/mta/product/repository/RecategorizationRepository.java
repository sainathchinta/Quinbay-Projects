package com.gdn.mta.product.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import com.gdn.mta.product.entity.Recategorization;

/**
 * Created by hardikbohra on 10/05/18.
 */
public interface RecategorizationRepository extends JpaRepository<Recategorization, String> {
}

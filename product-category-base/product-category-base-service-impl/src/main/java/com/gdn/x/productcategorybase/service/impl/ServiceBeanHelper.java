package com.gdn.x.productcategorybase.service.impl;

import org.springframework.data.jpa.repository.JpaRepository;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.entity.GdnBaseEntity;

public class ServiceBeanHelper {

  public static <T extends GdnBaseEntity> void deleteEntity(String id, JpaRepository<T, String> repository) {
    if ((id == null) || (repository.findById(id).orElse(null) == null)) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
          "can not delete un existence data with id : " + id);
    }
    T savedCatalog = repository.findById(id).orElse(null);
    repository.delete(savedCatalog);
  }

  public static <T extends GdnBaseEntity> String saveEntity(T entity, JpaRepository<T, String> repository) {
    if ((entity.getId() != null) && (repository.findById(entity.getId()).orElse(null) != null)) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_ACCESS,
          "use update for existing entity with id : " + entity.getId());
    }
    return repository.saveAndFlush(entity).getId();
  }

  public static <T extends GdnBaseEntity> void updateEntity(T entity, JpaRepository<T, String> repository) {
    if ((entity.getId() == null) || (repository.findById(entity.getId()).orElse(null) == null)) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
          "can not update un existence data with id : " + entity.getId());
    }
    repository.saveAndFlush(entity);
  }
}

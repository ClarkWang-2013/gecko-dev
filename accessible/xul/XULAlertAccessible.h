/* -*- Mode: C++; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#ifndef mozilla_a11y_XULAlertAccessible_h__
#define mozilla_a11y_XULAlertAccessible_h__

#include "AccessibleWrap.h"

namespace mozilla {
namespace a11y {

/**
 * Accessible for supporting XUL alerts.
 */

class XULAlertAccessible : public AccessibleWrap
{
public:
  XULAlertAccessible(nsIContent* aContent, DocAccessible* aDoc);

  NS_DECL_ISUPPORTS_INHERITED

  // Accessible
  virtual mozilla::a11y::ENameValueFlag Name(nsString& aName) MOZ_OVERRIDE;
  virtual a11y::role NativeRole() MOZ_OVERRIDE;
  virtual uint64_t NativeState() MOZ_OVERRIDE;

  // Widgets
  virtual bool IsWidget() const MOZ_OVERRIDE;
  virtual Accessible* ContainerWidget() const MOZ_OVERRIDE;

protected:
  ~XULAlertAccessible();
};

} // namespace a11y
} // namespace mozilla

#endif

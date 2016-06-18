/*
 * Upgrades.c
 *
 * This module handles translation of the database and preferences formats
 * between the different versions of MiniNav
 * 
 * (c) 2003 Blackhawk Systems Ltd
 * 
 */

#define DO_NOT_ALLOW_ACCESS_TO_INTERNALS_OF_STRUCTS
#include <BuildDefines.h>
#ifdef DEBUG_BUILD
#define ERROR_CHECK_LEVEL ERROR_CHECK_FULL
#endif

#include "Platform.h"
#include "ResourceDefines.h"
#include "Upgrades.h"
#include "GlobalTypes.h"
#include "CpInterface.h"
#include "Gps.h"

/****************************************************************************
 *
 * Public functions
 *
 *
 */


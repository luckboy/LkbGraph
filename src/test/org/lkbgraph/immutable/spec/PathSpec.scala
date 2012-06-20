/*******************************************************************************
 * Copyright (C) 2012 Łukasz Szpakowski.
 * 
 * This library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

package org.lkbgraph.immutable.spec
import org.scalatest.Spec
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.lkbgraph._
import org.lkbgraph.immutable._

@RunWith(classOf[JUnitRunner])
class PathSpec extends Spec with PathBehaviors[Path]
{
  override def pathFactory: PathFactory[Path] = Path
  
  describe("A Path") {
    it should behave like path
  }
}
